open Core
open Hardcaml
open Advent_of_fpga.Day1p1

let day1p1_testbench (sim : (_ I.t, _ O.t) Cyclesim.t) =
  let inputs, outputs = (Cyclesim.inputs sim, Cyclesim.outputs sim) in

  let ic = In_channel.open_text "day1p1.txt" in
  (try
     while true do
       let line = In_channel.input_line_exn ic in
       let processed = String.strip line in

       (* direction *)
       if Char.equal processed.[0] 'l' then
         inputs.dirL := Bits.vdd
       else
         inputs.dirL := Bits.gnd;

       (* distance *)
       let sub =
         String.sub processed ~pos:1 ~len:(String.length processed - 1)
       in
       let dist = Int.of_string sub in
       inputs.distance :=
         Bits.of_int ~width:(Bits.width !(inputs.distance)) dist;

       inputs.valid := Bits.vdd;
       Cyclesim.cycle sim
     done
   with End_of_file -> In_channel.close ic);

  (* deassert valid *)
  inputs.valid := Bits.gnd;

  let result = Bits.to_unsigned_int !(outputs.count) in
  Stdio.print_s [%message (result : int)]

let test () =
  let module Sim = Cyclesim.With_interface (Day1p1.I) (Day1p1.O) in
  let sim = Sim.create Day1p1.create in
  let waves, sim = Waveform.create sim in
  day1p1_testbench sim;
  waves
