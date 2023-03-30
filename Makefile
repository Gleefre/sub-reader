LISP ?= sbcl
APP = sub-reader

all: clean build

build:
	$(LISP) --eval "(ql:quickload :deploy)" \
		--load $(APP).asd \
		--eval "(ql:quickload :$(APP))" \
		--eval "(deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)" \
		--eval "(deploy:define-resource-directory data \"res/\")" \
		--eval "(asdf:make :$(APP))" \
		--quit

clean:
	rm -rf $(APP)
	rm -rf bin
	rm -rf $(APP)-win.zip
	rm -rf $(APP)-lin.zip
	rm -rf $(APP)-mac.zip

lin-bundle: all
	mkdir $(APP)
	mv bin $(APP)
	cp run.sh $(APP)
	cp LICENSE $(APP)
	zip -r $(APP)-lin $(APP)

win-bundle: all
	mkdir $(APP)
	mv bin $(APP)/
	cp run.bat $(APP)
	cp LICENSE $(APP)

mac-bundle: all
	mv bin $(APP)
	cp LICENSE $(APP)
	zip -r $(APP)-mac $(APP)
