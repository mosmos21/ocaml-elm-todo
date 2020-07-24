server:
	rm -rf _build/
	ocamlbuild -use-ocamlfind -tag thread -pkg cohttp-lwt-unix,ssl,lwt_ssl,mysql,routes,yojson,logs -build-dir _build -I backend/src main.native
	