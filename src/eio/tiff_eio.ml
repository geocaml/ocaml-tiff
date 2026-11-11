(* Provides utilities for working with TIFF files using the Eio library *)

let with_open_in path fn =
  Eio.Path.with_open_in path @@ fun r ->
  let ro = Eio.File.pread_exact r in
  fn ro

let with_open_out ?(open_flags = `If_missing 0o644) path fn =
  match open_flags with
  | `Exclusive _ -> (
      let created = ref false in
      try
        Eio.Path.with_open_out ~create:open_flags path @@ fun w ->
        created := true;
        let wo = Eio.File.pwrite_all w in
        fn wo
      with exn ->
        (if !created then try Eio.Path.unlink path with _ -> ());
        raise exn)
  | `If_missing perm -> (
      let created = ref false in
      let write create =
        try
          Eio.Path.with_open_out ~create path @@ fun w ->
          if create = `Exclusive perm then created := true;
          let wo = Eio.File.pwrite_all w in
          fn wo
        with exn ->
          (if !created then try Eio.Path.unlink path with _ -> ());
          raise exn
      in
      try write (`Exclusive perm)
      with
      | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) when not !created ->
        write `Never)
  | `Or_truncate perm -> (
      let created = ref false in
      let write create =
        try
          Eio.Path.with_open_out ~create path @@ fun w ->
          if create = `Exclusive perm then created := true;
          if create = `Never then Eio.File.truncate w Optint.Int63.zero;
          let wo = Eio.File.pwrite_all w in
          fn wo
        with exn ->
          (if !created then try Eio.Path.unlink path with _ -> ());
          raise exn
      in
      try write (`Exclusive perm)
      with
      | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) when not !created ->
        write `Never)
  | _ ->
      Eio.Path.with_open_out ~create:open_flags path @@ fun w ->
      let wo = Eio.File.pwrite_all w in
      fn wo
