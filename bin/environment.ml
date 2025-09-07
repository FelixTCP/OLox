module Environment = struct
  open Value

  type t = (string, Value.lox_value) Hashtbl.t

  let create () = Hashtbl.create 16
  let define env name value = Hashtbl.replace env name value
  let get env name = try Some (Hashtbl.find env name) with Not_found -> None
end
