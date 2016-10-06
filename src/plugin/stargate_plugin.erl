-module(stargate_plugin).
-compile(export_all).

serve_static(Base, Path, Headers, S) -> stargate_static_file:serve_static(Base, Path, Headers, S).
serve_static_bin(Bin, Headers, S) -> stargate_static_file:serve_static_bin(Bin, Headers, S).


%{text, Payload}
%{bin, Payload}
%{text_compress, Payload}
%{bin_compress, Payload}
ws_send(VesselPid, {Type, Payload}) -> VesselPid ! {ws_send, {Type, Payload}}.
ws_message(VesselPid, Msg) -> VesselPid ! {ws_message, Msg}.