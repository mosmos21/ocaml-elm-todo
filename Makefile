server:
	rm -rf _build/
	ocamlbuild -use-ocamlfind -tag thread -pkg cohttp-lwt-unix,ssl,lwt_ssl,mysql,routes,yojson,logs -build-dir _build -I backend/src main.native
	
run-front:
	cd frontend && elm-live src/Main.elm \
		--port=3000 \
		--start-page=index.html \
		--proxy-host=http://localhost:8000/api \
		--proxy-prefix=/api \
		-- --output=assets/index.js

front:
	cd frontend && \
	elm make src/Main.elm --optimize --output=assets/index.js