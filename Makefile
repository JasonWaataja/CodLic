INSTALL_PREFIX ?= /usr/local
INSTALL_BIN = ${INSTALL_PREFIX}/bin
INSTALL_IMAGE = ${INSTALL_PREFIX}/lib/common-lisp/images
CL_LAUNCH ?= ./cl-launch.sh
EXEC=codlic
IMAGE=${EXEC}.image
EVAL_FORM = (defparameter *install-prefix* "${INSTALL_PREFIX}") \
			(load "setup.lisp")
CURRENT_DIR = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

install_image:
	mkdir -p ${INSTALL_IMAGE}
	${CL_LAUNCH} -e '${EVAL_FORM}' -d "${INSTALL_IMAGE}/${IMAGE}"

install: install_image
	mkdir -p ${INSTALL_BIN}
	${CL_LAUNCH} -m ${INSTALL_IMAGE}/${IMAGE} -p codlic \
		-E main -o ${INSTALL_BIN}/${EXEC}
	install -Dm755 licenses/gplv3 ${INSTALL_PREFIX}/share/codlic/licenses/gplv3
	install -Dm755 licenses/mit ${INSTALL_PREFIX}/share/codlic/licenses/mit
	install -Dm644 man/codlic.1 ${INSTALL_PREFIX}/share/man/man1

# Requires the qlot program. Updates the libraries stored in the quicklisp
# directory.
update_dependencies:
	qlot update
