(** Error categories for TIFF operations. *)
type kind =
  | Invalid_argument
  | Unsupported_feature
  | Malformed_file
  | Internal_error

(** A TIFF-specific exception containing the error category,
    descriptive message, and optional filename. *)  

exception Tiff_error of kind * string * string option

let raise_error ?filename kind message =
  raise (Tiff_error (kind, message, filename))

let raisef ?filename kind fmt =
  Fmt.kstr (raise_error ?filename kind) fmt  

(* Convert an error category into a readable label *)
let string_of_kind = function
  | Invalid_argument -> "Invalid argument"
  | Unsupported_feature -> "Unsupported feature"
  | Malformed_file -> "Malformed file"
  | Internal_error -> "Internal error"

(* Pretty-print a structured TIFF error *)
let pp fmt (kind, message, filename) =
  match filename with
  | Some name ->
      Format.fprintf fmt
        "TIFF error (%s) in %s: %s"
        (string_of_kind kind) name message
  | None ->
      Format.fprintf fmt
        "TIFF error (%s): %s"
        (string_of_kind kind) message

(* Register a printer for TIFF-specific exceptions *)
let () =
  Printexc.register_printer (function
    | Tiff_error (kind, message, filename) ->
        Some (Format.asprintf "%a" pp (kind, message, filename))
    | _ -> None)
 