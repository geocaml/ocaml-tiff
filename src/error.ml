let invalid_arg message =
  Stdlib.invalid_arg message

let invalid_argf fmt =
  Fmt.kstr invalid_arg fmt  
  