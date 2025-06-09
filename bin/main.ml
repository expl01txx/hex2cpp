let read_whole_file filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  buf

let write_whole_file filename data =
  let oc = open_out_bin filename in
  output oc data 0 (Bytes.length data);
  close_out oc

let bytes_to_hex_string bytes row_size =
  let buf = Buffer.create (Bytes.length bytes * 5) in  (* Approximate size *)
  for i = 0 to Bytes.length bytes - 1 do
    if i > 0 then Buffer.add_char buf ' ';
    if i mod row_size == 0 then Buffer.add_string buf "\n    ";
    Printf.bprintf buf "0x%02x," (Char.code (Bytes.get bytes i))
  done;
  Buffer.contents buf
  
let () =
  if Array.length Sys.argv <= 1 then (
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <path to file> <number?>"); 
    exit(2)
  );;
  
  let filename = Sys.argv.(1) in    
  print_endline ("[hex2cpp]: Reading: " ^ filename);

  let data = bytes_to_hex_string(read_whole_file filename) 16 in

  print_endline "[hex2cpp]: Generating code...";
  let payload = "/*\nGenerated using Hex2Cpp by Expl01t\n*/\nunsigned char bytes[] = {" ^ data ^ "\n};\n" in

  print_endline "[hex2cpp]: Writing to bytes.cpp";  
  write_whole_file "bytes.cpp" (Bytes.of_string payload)
  
    
  
  
  
