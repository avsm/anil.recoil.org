open Printf

module Index = struct

  let t =
    match Static_templates.t "tmpl.inc" with 
    | Some b ->
       b

end
