open Base
open Hardcaml
open Hardcaml.Signal

let bit = 32

module I = struct
  type 'a t = {
    clock : 'a;
    clear : 'a;
    valid : 'a;
    distance : 'a; [@bits bit]
    dirL : 'a;
  }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { count : 'a [@bits bit] } [@@deriving hardcaml]
end

module States = struct
  type t =
    | S_idle
    | S_count
    | S_inc
    | S_result
  [@@deriving sexp_of, compare, enumerate]
end

let create (i : _ I.t) =
  let r_sync = Signal.Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
  let count = Always.Variable.reg ~enable:vdd rsync ~width:bit in
  let lock = Always.Variable.reg ~enable:vdd rsync ~width:bit in
  Always.(
    compile
      [
        sm.switch
          [
            ( S_idle,
              [
                lock <--. 50;
                if_ i.valid [ sm.set_next S_count ] [ sm.set_next S_result ];
              ] );
            ( S_count,
              [
                if_ dirL
                  [ lock <-- lock.value -:. i.distance ]
                  [ lock <-- lock.value +:. i.distance ];
                sm.set_next S_inc;
              ] );
            ( S_inc,
              [
                if_ lock.value
                ==:. 0
                       [ count.value <-- count.value +:. 1 ]
                       [
                         if_ i.valid
                           [ sm.set_next S_count ]
                           [ sm.set_next S_result ];
                       ];
              ] );
            (S_result, [ sm.set_next S_idle ]);
          ];
      ]);
  { O.count = count.value }
