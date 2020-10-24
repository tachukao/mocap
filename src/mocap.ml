open Base
module Vec = Vec
module Quaternion = Quaternion

type 'a xyz = 'a * 'a * 'a

let deg_to_rad x = Owl.Const.pi *. x /. 180.

module Joint = struct
  type t =
    { name : string
    ; length : float
    ; direction : float xyz
    ; axis : float xyz
    ; dof : (float * float) option xyz
    ; q : Quaternion.t
    }
end

module Coord = struct
  type t =
    { position : float xyz
    ; q : Quaternion.t
    }

  let process ~root ~joints ~hierarchy motion =
    let tbl = Hashtbl.create (module String) in
    let rec process = function
      | [] -> tbl
      | ((parent : t), (children : string list)) :: tl ->
        (match children with
        | [] -> process tl
        | child :: others ->
          assert (not String.(child = "root"));
          let m =
            match Hashtbl.find motion child with
            | Some m -> m |> Array.of_list
            | None -> [| 0.; 0.; 0. |]
          in
          let j = Hashtbl.find_exn joints child in
          let rotation =
            (match Joint.(j.dof) with
            | Some _, Some _, Some _ -> m.(0), m.(1), m.(2)
            | Some _, Some _, None -> m.(0), m.(1), 0.
            | Some _, None, Some _ -> m.(0), 0., m.(1)
            | Some _, None, None -> m.(0), 0., 0.
            | None, Some _, Some _ -> 0., m.(0), m.(1)
            | None, Some _, None -> 0., m.(0), 0.
            | None, None, Some _ -> 0., 0., m.(0)
            | None, None, None -> 0., 0., 0.)
            |> Vec.map ~f:deg_to_rad
          in
          let q = Quaternion.(parent.q * j.q * of_sxyz rotation * conj j.q) in
          let position =
            Vec.(parent.position + (j.length $* Quaternion.(rotate q j.direction)))
          in
          Hashtbl.add_exn ~key:child ~data:position tbl;
          (match Hashtbl.find hierarchy child with
          | Some grandchildren ->
            process ((parent, others) :: ({ position; q }, grandchildren) :: tl)
          | None -> process ((parent, others) :: tl)))
    in
    let root =
      let m = Hashtbl.find_exn motion "root" |> Array.of_list in
      let translation = m.(0), m.(1), m.(2) in
      let rotation = (m.(3), m.(4), m.(5)) |> Vec.map ~f:deg_to_rad in
      let position = Vec.(translation + root.position) in
      Hashtbl.add_exn ~key:"root" ~data:position tbl;
      let q = Quaternion.(root.q * of_sxyz rotation * conj root.q) in
      { position; q }
    in
    let children = Hashtbl.find_exn hierarchy "root" in
    process [ root, children ]


  let links ~hierarchy coordinates
      : ((string * (float * float * float)) * (string * (float * float * float))) list
    =
    Hashtbl.to_alist hierarchy
    |> List.fold ~init:[] ~f:(fun accu (parent, children) ->
           let parent_pos = Hashtbl.find_exn coordinates parent in
           List.fold children ~init:accu ~f:(fun accu2 child ->
               let x =
                 (parent, parent_pos), (child, Hashtbl.(find_exn coordinates child))
               in
               x :: accu2))
end

module Parser = struct
  let meta =
    let open Re in
    let segment s = [ bol; char ':'; word (str s); eow; group (rep1 any) ] |> seq in
    let re =
      [ segment "version"
      ; segment "name"
      ; segment "units"
      ; segment "documentation"
      ; segment "root"
      ; segment "bonedata"
      ; segment "hierarchy"
      ]
      |> seq
    in
    fun data ->
      let m = exec (compile re) data in
      let root, bonedata, hierarchy = Group.get m 5, Group.get m 6, Group.get m 7 in
      root, bonedata, hierarchy


  let root =
    let open Re in
    let segment s = [ bow; word (str s); eow; group (rep1 any) ] |> seq in
    let re = [ segment "position"; segment "orientation" ] |> seq in
    let extract x =
      x
      |> String.strip
      |> String.split ~on:' '
      |> List.map ~f:Float.of_string
      |> Array.of_list
      |> fun x -> x.(0), x.(1), x.(2)
    in
    fun data ->
      let m = exec (compile re) data in
      let position = Group.(get m 1) |> extract in
      let orientation = Group.(get m 2) |> extract |> Vec.map ~f:deg_to_rad in
      Coord.{ position; q = Quaternion.(of_sxyz orientation) }


  let bonedata =
    let open Re in
    let segment s = [ bow; word (str s); eow; group (rep1 any) ] |> seq in
    let final = [ bow; word (str "end"); eow ] |> seq in
    let re =
      [ segment "name"
      ; segment "direction"
      ; segment "length"
      ; segment "axis"
      ; (* rest *)
        eol
      ; group (rep1 any)
      ; final
      ]
      |> seq
      |> shortest
      |> compile
    in
    let rest_re = [ segment "dof"; segment "limits" ] |> seq |> compile in
    fun data ->
      let m = all re data in
      List.map m ~f:(fun x ->
          let name = Group.(get x 1 |> String.strip) in
          let direction =
            let ds =
              Group.(get x 2 |> String.strip)
              |> String.split ~on:' '
              |> List.map ~f:Float.of_string
              |> Array.of_list
            in
            ds.(0), ds.(1), ds.(2)
          in
          let length = Group.(get x 3 |> String.strip) |> Float.of_string in
          let axis =
            let a =
              Group.(get x 4 |> String.strip) |> String.split ~on:' ' |> Array.of_list
            in
            (Float.of_string a.(0), Float.of_string a.(1), Float.of_string a.(2))
            |> Vec.map ~f:deg_to_rad
          in
          let rest = Group.(get x 5 |> String.strip) |> exec_opt rest_re in
          let dof =
            match rest with
            | None -> None, None, None
            | Some s ->
              let rs =
                Group.get s 1
                |> String.strip
                |> String.split ~on:' '
                |> List.map ~f:String.strip
              in
              let limits =
                Group.get s 2
                |> String.strip
                |> String.split ~on:'\n'
                |> List.map ~f:(fun x ->
                       x
                       |> String.strip
                       |> String.strip ~drop:(fun x -> Char.(x = '(' || x = ')'))
                       |> String.split ~on:' '
                       |> List.map ~f:Float.of_string
                       |> Array.of_list
                       |> fun x -> x.(0), x.(1))
              in
              assert (List.length rs = List.length limits);
              let combined = List.zip_exn rs limits in
              ( List.Assoc.find combined ~equal:String.equal "rx"
              , List.Assoc.find combined ~equal:String.equal "ry"
              , List.Assoc.find combined ~equal:String.equal "rz" )
          in
          let q = Quaternion.of_sxyz axis in
          name, Joint.{ name; direction; length; axis; q; dof })
      |> Hashtbl.of_alist_exn (module String)


  let hierarchy data =
    let tbl = Hashtbl.create (module String) in
    data
    |> String.strip
    |> String.split_lines
    |> List.iter ~f:(fun s ->
           let s = String.strip s in
           if not String.(s = "begin" || s = "end")
           then (
             let l = String.split ~on:' ' s in
             let k = List.hd_exn l in
             let tl = List.tl_exn l in
             Hashtbl.add_exn tbl ~key:k ~data:tl));
    tbl


  let amc data =
    let data = String.strip data |> String.split_lines in
    let accu, buf, _ =
      List.fold data ~init:([], [], false) ~f:(fun (accu, buf, write) x ->
          let x = String.strip x in
          try
            let _ = Int.of_string x in
            match buf with
            | [] -> accu, [], true
            | x ->
              let tbl = Hashtbl.of_alist_exn (module String) x in
              tbl :: accu, [], true
          with
          | Failure _ ->
            if write
            then (
              let x = String.split ~on:' ' x in
              let hd = List.hd_exn x in
              let tl =
                List.tl_exn x
                |> List.map ~f:(fun x -> x |> String.strip |> Float.of_string)
              in
              accu, (hd, tl) :: buf, write)
            else accu, buf, write)
    in
    match buf with
    | [] -> accu |> List.rev
    | x ->
      let tbl = Hashtbl.of_alist_exn (module String) x in
      tbl :: accu |> List.rev
end

let read_asf file =
  let asf = Stdio.In_channel.read_all file in
  let root, bonedata, hierarchy = Parser.meta asf in
  let root = Parser.root root in
  let joints = Parser.bonedata bonedata in
  let hierarchy = Parser.hierarchy hierarchy in
  root, joints, hierarchy


let read_amc file = Stdio.In_channel.read_all file |> Parser.amc
