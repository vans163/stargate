-module(stargate_plugin).
-compile(export_all).

serve_static(Path, Headers, S) -> stargate_static_file:serve_static(Path, Headers, S).