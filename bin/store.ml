open Petrol
open Petrol.Postgres

let current_version = VersionedSchema.version [ 1; 0; 0 ]
let generate_id = Uuidm.v4_gen (Random.State.make_self_init ())

let make_salt size =
  String.init size (fun _ -> Random.int_in_range ~min:0 ~max:255 |> Char.chr)
;;

let hash_password password =
  let salt = make_salt 20 in
  let encoded_len =
    Argon2.encoded_len
      ~t_cost:2
      ~m_cost:65536
      ~parallelism:1
      ~salt_len:20
      ~hash_len:32
      ~kind:Argon2.ID
  in
  Argon2.hash
    ~t_cost:2
    ~m_cost:65536
    ~parallelism:1
    ~pwd:password
    ~salt
    ~kind:Argon2.ID
    ~hash_len:32
    ~encoded_len
    ~version:Argon2.VERSION_13
;;

let check_password dbp up = Argon2.verify ~encoded:dbp ~pwd:up ~kind:Argon2.ID

module Database = struct
  exception NotInitialized
  exception ConnectionUnavailable of string

  let connect uri = Caqti_lwt_unix.connect (Uri.of_string uri)
  let schema = VersionedSchema.init current_version ~name:"twixt"

  let connect_exn uri =
    let conn_promise = connect uri in
    match Lwt_main.run conn_promise with
    | Error err -> raise (ConnectionUnavailable (Caqti_error.show err))
    | Ok module_ -> module_
  ;;

  let d = ref None

  let init uri =
    d := Some (connect_exn uri);
    Petrol.VersionedSchema.initialise schema (Option.get !d)
  ;;

  let get () =
    match !d with
    | Some x -> x
    | None -> raise NotInitialized
  ;;
end

module User = struct
  module S = struct
    let table, Expr.[ id; username; email; password; elo; deleted ] =
      VersionedSchema.declare_table
        Database.schema
        ~name:"user"
        Schema.
          [ field "id" ~ty:Type.text ~constraints:[ primary_key () ]
          ; field "username" ~ty:Type.text
          ; field "email" ~ty:Type.text
          ; field "password" ~ty:Type.text
          ; field "elo" ~ty:Type.int
          ; field "deleted" ~ty:Type.bool
          ]
    ;;
  end

  type t =
    { id : Uuidm.t
    ; username : string
    ; email : string
    ; elo : int
    ; deleted : bool
    }

  let create ~username:uname ~email:eml ~password:pwd =
    let uid = generate_id () in
    let hashed_password =
      match hash_password pwd with
      | Result.Ok (_, encoded) -> encoded
      | Result.Error _ -> failwith "failed to create password"
    in
    Query.insert
      ~table:S.table
      ~values:
        Expr.
          [ S.id := s (Uuidm.to_string uid)
          ; S.username := s uname
          ; S.email := s eml
          ; S.password := s hashed_password
          ; S.elo := i Serverconfig.default_elo
          ; S.deleted := bl false
          ]
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let delete ~user_id =
    Query.update
      ~table:S.table
      ~set:
        Expr.
          [ S.username := s ("Deleted " ^ make_salt 6)
          ; S.email := s "deleted@deleted.deleted"
          ; S.password := s "deleted"
          ; S.elo := i 0
          ; S.deleted := bl true
          ]
    |> Query.where Expr.(S.id = s (Uuidm.to_string user_id))
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let update_profile ~user_id ~username:nname =
    Query.update ~table:S.table ~set:Expr.[ S.username := s nname ]
    |> Query.where Expr.(S.id = s (Uuidm.to_string user_id))
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let update_elo ~user_id ~elo:newelo =
    Query.update ~table:S.table ~set:Expr.[ S.elo := i newelo ]
    |> Query.where Expr.(S.id = s (Uuidm.to_string user_id))
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let update_password ~user_id ~password:pwd =
    let hashed_password =
      match hash_password pwd with
      | Result.Ok (_, encoded) -> encoded
      | Error _ -> failwith "failed to hash"
    in
    Query.update ~table:S.table ~set:Expr.[ S.password := s hashed_password ]
    |> Query.where Expr.(S.id = s (Uuidm.to_string user_id))
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let user_password_check ~user_id ~password:pwd =
    Query.select Expr.[ S.password ] ~from:S.table
    |> Query.where Expr.(S.id = s (Uuidm.to_string user_id))
    |> Request.make_one
    |> Petrol.find (Database.get ())
    |> Lwt_result.map (fun (true_password, ()) ->
      match check_password true_password pwd with
      | Result.Ok v -> v
      | Result.Error _ -> failwith "failed to check password :/")
  ;;

  let fetch ~user_id =
    Query.select Expr.[ S.username; S.email; S.elo; S.deleted ] ~from:S.table
    |> Query.where Expr.(S.id = s (Uuidm.to_string user_id))
    |> Request.make_one
    |> Petrol.find (Database.get ())
    |> Lwt_result.map (fun (username, (email, (elo, (deleted, ())))) ->
      { id = user_id; username; email; elo; deleted })
  ;;
end

module Session = struct
  type t =
    { user_id : Uuidm.t
    ; sess_id : Uuidm.t
    ; current_rotation : int
    ; revoked : bool
    }

  module S = struct
    let table, Expr.[ user_id; sess_id; current_rotation; revoked ] =
      VersionedSchema.declare_table
        Database.schema
        ~name:"session"
        Schema.
          [ field "sess_id" ~ty:Type.text ~constraints:[ primary_key () ]
          ; field
              "user_id"
              ~ty:Type.text
              ~constraints:
                [ foreign_key ~table:User.S.table ~columns:Expr.[ User.S.id ] () ]
          ; field "current_rotation" ~ty:Type.int
          ; field "revoked" ~ty:Type.bool
          ]
    ;;
  end

  let create ~user_id ~sess_id =
    Query.insert
      ~table:S.table
      ~values:
        Expr.
          [ S.user_id := s (Uuidm.to_string user_id)
          ; S.sess_id := s (Uuidm.to_string sess_id)
          ; S.current_rotation := i 0
          ; S.revoked := bl false
          ]
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let rotate ~sess_id =
    Query.update
      ~table:S.table
      ~set:Expr.[ S.current_rotation := S.current_rotation + i 1 ]
    |> Query.where Expr.(S.sess_id = s (Uuidm.to_string sess_id))
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let revoke ~sess_id =
    Query.update ~table:S.table ~set:Expr.[ S.revoked := bl true ]
    |> Query.where Expr.(S.sess_id = s (Uuidm.to_string sess_id))
    |> Request.make_zero
    |> Petrol.exec (Database.get ())
  ;;

  let fetch ~sess_id =
    Query.select Expr.[ S.user_id; S.current_rotation; S.revoked ] ~from:S.table
    |> Query.where Expr.(S.sess_id = s (Uuidm.to_string sess_id))
    |> Request.make_one
    |> Petrol.find (Database.get ())
    |> Lwt_result.map (fun (user_id, (current_rotation, (revoked, ()))) ->
      { sess_id
      ; user_id = Option.get (Uuidm.of_string user_id)
      ; current_rotation
      ; revoked
      })
  ;;
end

module Game = struct
  type status_t =
    | BWin
    | RWin
    | Running
    | Waiting

  type t =
    { id : Uuidm.t
    ; red : Uuidm.t (* red player user id *)
    ; black : Uuidm.t (* black player user id *)
    ; repr : string
    ; status : status_t
    ; resigned : bool
    ; size : int
    }

  let create ~id ~red ~black = ()

  let fetch id =
    Some
      { id
      ; red = Uuidm.v4_gen (Random.State.make_self_init ()) ()
      ; black = Uuidm.v4_gen (Random.State.make_self_init ()) ()
      ; repr = ""
      ; status = Waiting
      ; resigned = false
      ; size = 24
      }
  ;;
end

module ChatEntry = struct
  type t =
    { user_id : Uuidm.t
    ; message : string
    ; game_id : Uuidm.t
    ; sent_at : int
    }

  let create ~user_id ~message ~game_id ~sent_at = ()
end
