open Cryptokit
open Ocaml_twixt_exchange.Uuidm_ex

module Jwt = struct
  let compute_signature header payload secret =
    let theader = Base64.encode_compact () in
    let tpayload = Base64.encode_compact () in
    let hsh = MAC.hmac_sha256 secret in
    hash_string
      hsh
      (transform_string theader header ^ "." ^ transform_string tpayload payload)
    |> transform_string (Base64.encode_compact ())
  ;;

  let make header payload secret =
    let theader = Base64.encode_compact () in
    let tpayload = Base64.encode_compact () in
    let signature = compute_signature header payload secret in
    transform_string theader header
    ^ "."
    ^ transform_string tpayload payload
    ^ "."
    ^ signature
  ;;

  let is_valid token secret =
    let slices = String.split_on_char '.' token in
    match slices with
    | [ header; payload; signature ] ->
      let expected_signature = compute_signature header payload secret in
      String.equal expected_signature signature
    | _ -> false
  ;;

  let parse_opt token secret =
    if not (is_valid token secret)
    then None
    else (
      match String.split_on_char '.' token with
      | [ _; payload; _ ] ->
        let decoder = Base64.decode () in
        Some (transform_string decoder payload |> Yojson.Safe.from_string)
      | _ -> None)
  ;;
end

module SessionToken = struct
  let header = "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"

  type token_type_t =
    | Access
    | Refresh
  [@@deriving yojson]

  type payload_t =
    { token_type : token_type_t
    ; expires_at : int
    ; sess_id : Uuidm.t
    ; user_id : Uuidm.t
    ; rot : int
    }
  [@@deriving yojson]

  type result_t =
    | Malformed
    | Expired
    | Ok of payload_t

  let create_token_pair ~access_ttl ~refresh_ttl ~sess_id ~user_id ~secret ~rotation =
    let access_payload =
      { token_type = Access; expires_at = access_ttl; sess_id; user_id; rot = 0 }
      |> payload_t_to_yojson
      |> Yojson.Safe.to_string
    in
    let refresh_payload =
      { token_type = Refresh; expires_at = refresh_ttl; rot = rotation; sess_id; user_id }
      |> payload_t_to_yojson
      |> Yojson.Safe.to_string
    in
    Jwt.make header access_payload secret, Jwt.make header refresh_payload secret
  ;;

  let parse_token token secret =
    match Jwt.parse_opt token secret with
    | Some tbd ->
      (match payload_t_of_yojson tbd with
       | Ok payload ->
         if payload.expires_at <= Cache.int_time () then Expired else Ok payload
       | Error _ -> Malformed)
    | None -> Malformed
  ;;
end
