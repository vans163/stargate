-module(stargate_plugin).
-compile(export_all).

serve_static(Base, Path, Headers, S) -> stargate_static_file:serve_static(Base, Path, Headers, S).


%{text, Payload}
%{bin, Payload}
%{text_compress, Payload}
%{bin_compress, Payload}
ws_send(VesselPid, P) -> VesselPid ! {ws_send, P}.
ws_message(VesselPid, Msg) -> VesselPid ! {ws_message, Msg}.
