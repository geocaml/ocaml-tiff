type kind =
  | Invalid_argument
  | Unsupported_feature
  | Malformed_file
  | Internal_error

exception Tiff_error of kind * string * string option

let raise_error ?filename kind message =
  raise (Tiff_error (kind, message, filename))

let raisef ?filename kind fmt =
  Fmt.kstr (raise_error ?filename kind) fmt  

(*Keeping old functions as is for now*)
let invalid_arg message =
  Stdlib.invalid_arg message

let invalid_argf fmt =
  Fmt.kstr invalid_arg fmt  
