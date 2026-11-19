let rec lookup env chave =
  match env with
  | [] -> None
  | (x, t) :: resto ->
      if x = chave then Some t
      else lookup resto chave;;


let update env chave t =
  (chave, t) :: env;;


