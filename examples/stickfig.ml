open Base
open Mocap

let () =
  let root, joints, hierarchy = read_asf "examples/07.asf" in
  let motions = read_amc "examples/07_07.amc" in
  let nodes =
    List.map ~f:(fun motion -> Coord.process ~root ~joints ~hierarchy motion) motions
  in
  Stdio.Out_channel.with_file "nodes" ~f:(fun out ->
      List.iter nodes ~f:(fun nodes ->
          nodes
          |> Hashtbl.to_alist
          |> List.map ~f:(fun (name, (x, y, z)) ->
                 Printf.sprintf "%s %f %f %f" name x y z)
          |> Stdio.Out_channel.output_lines out;
          Stdio.Out_channel.output_string out "\n"));
  Stdio.Out_channel.with_file "links" ~f:(fun out ->
      List.iter nodes ~f:(fun nodes ->
          Coord.links ~hierarchy nodes
          |> List.map ~f:(fun ((name1, (x1, y1, z1)), (name2, (x2, y2, z2))) ->
                 Printf.sprintf "%s %f %f %f %s %f %f %f" name1 x1 y1 z1 name2 x2 y2 z2)
          |> Stdio.Out_channel.output_lines out;
          Stdio.Out_channel.output_string out "\n"))
