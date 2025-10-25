// C stubs for the OCaml TIFF decoder for performing low-level systems operations

/*

From Eio.

Copyright (C) 2021 Anil Madhavapeddy  
Copyright (C) 2022 Thomas Leonard

Permission to use, copy, modify, and distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE. */
#define _FILE_OFFSET_BITS 64

#include <sys/types.h>
#include <sys/socket.h>
#ifdef __linux__
#if __GLIBC__ > 2 || __GLIBC_MINOR__ > 24
#include <sys/random.h>
#else
#include <sys/syscall.h>
#endif
#endif
#include <sys/uio.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/socketaddr.h>
#include <caml/custom.h>
#include <caml/fail.h>

#ifdef ARCH_SIXTYFOUR
#define Int63_val(v) Long_val(v)
#define caml_copy_int63(v) Val_long(v)
#else
#define Int63_val(v) (Int64_val(v)) >> 1
#define caml_copy_int63(v) caml_copy_int64(v << 1)
#endif

static void caml_stat_free_preserving_errno(void *ptr) {
  int saved = errno;
  caml_stat_free(ptr);
  errno = saved;
}

/* Allocates an array of C iovecs using the cstructs in the array [v_bufs]. */
static struct iovec *alloc_iov(value v_bufs) {
  struct iovec *iov;
  int n_bufs = Wosize_val(v_bufs);

  if (n_bufs == 0) return NULL;
  iov = caml_stat_calloc_noexc(n_bufs, sizeof(struct iovec));
  if (iov == NULL)
    caml_raise_out_of_memory();

  for (int i = 0; i < n_bufs; i++) {
    value v_cs = Field(v_bufs, i);
    value v_ba = Field(v_cs, 0);
    value v_off = Field(v_cs, 1);
    value v_len = Field(v_cs, 2);
    iov[i].iov_base = (uint8_t *)Caml_ba_data_val(v_ba) + Long_val(v_off);
    iov[i].iov_len = Long_val(v_len);
  }
  return iov;
}

CAMLprim value caml_tiff_preadv(value v_fd, value v_bufs, value v_offset) {
  CAMLparam2(v_bufs, v_offset);
  ssize_t r;
  int n_bufs = Wosize_val(v_bufs);
  struct iovec *iov;
  off_t offset = Int63_val(v_offset);

  iov = alloc_iov(v_bufs);
  caml_enter_blocking_section();
  r = preadv(Int_val(v_fd), iov, n_bufs, offset);
  caml_leave_blocking_section();
  caml_stat_free_preserving_errno(iov);
  if (r < 0) uerror("preadv", Nothing);

  CAMLreturn(Val_long(r));
}
