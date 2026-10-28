open Cmdliner

let inspect_file path =
  if not (Sys.file_exists path) then (
     Printf.eprintf "[ERROR] File not found: %s\n" path
  ) else
    try
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () ->
           let len = in_channel_length ic in
           if len < 4 then
             raise (Tiff.Error.Tiff_error (Tiff.Error.Malformed_file, "File is too short to contain a valid TIFF header", Some path))
           else
             let header = really_input_string ic 4 in
             if header = "II*\000" || header = "MM\000*" then
               Printf.printf "=== tiff-traceback: %s ===\n[OK] Valid Standard TIFF header signature (%s).\n" path (String.escaped header)
             else if header = "II+\000" || header = "MM\000+" then
               raise (Tiff.Error.Tiff_error (Tiff.Error.Unsupported_feature, "BigTIFF structure detected (partial support)", Some path))
             else
               raise (Tiff.Error.Tiff_error (Tiff.Error.Malformed_file, "Unrecognized TIFF endianness or magic bytes", Some path)))
    with
    | Tiff.Error.Tiff_error (kind, msg, _filename) ->
        Printf.eprintf "=== tiff-traceback: %s ===\n" path;
        begin match kind with
        | Tiff.Error.Malformed_file ->
            Printf.eprintf "[ERROR] [Malformed File]: %s\n" msg
        | Tiff.Error.Unsupported_feature ->
            Printf.eprintf "[ERROR] [Unsupported Feature / Format Variant]: %s\n" msg
        | Tiff.Error.Invalid_argument ->
            Printf.eprintf "[ERROR] [Invalid Argument]: %s\n" msg
        | Tiff.Error.Internal_error ->
            Printf.eprintf "[ERROR] [Internal Error]: %s\n" msg
        end
    | Sys_error msg ->
        Printf.eprintf "=== tiff-traceback: %s ===\n[ERROR] [System / I/O Error]: %s\n" path msg
    | exn ->
        Printf.eprintf "=== tiff-traceback: %s ===\n[ERROR] [Unexpected Error]: %s\n" path (Printexc.to_string exn)

let inspect_files paths =
  List.iter inspect_file paths

let files_arg =
  let doc = "Paths to target TIFF images." in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"FILES" ~doc)

let cmd =
  let doc = "Diagnostic error reporting utility for ocaml-tiff" in
  let info = Cmd.info "tiff-traceback" ~version:"0.1.0" ~doc in
  Cmd.v info Term.(const inspect_files $ files_arg)

let () = exit (Cmd.eval cmd)
