type kind =
  | Invalid_argument
  | Unsupported_feature
  | Malformed_file
  | Internal_error

exception Tiff_error of kind * string

let raise_error kind message =
  raise (Tiff_error (kind, message))

let raisef kind fmt =
  Fmt.kstr (raise_error kind) fmt

(*Keeping old functions as is for now*)
let invalid_arg message =
  Stdlib.invalid_arg message

let invalid_argf fmt =
  Fmt.kstr invalid_arg fmt  
