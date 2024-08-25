SRC_DIR=src
BIN_FILE=buildfile
PARSING_FILES=shell(ls #SRC_DIR/parsing/*.rs)
EXECUTION_FILES=shell(ls #SRC_DIR/execution/*.rs)

RUST_FLAGS=--edition 2021 -C opt-level=3

ifdef $DEBUG
    #RUST_FLAGS-=-C opt-level=3
    #RUST_FLAGS+=--cfg='feature="dbg"'
endif

#BIN_FILE: #SRC_DIR/main.rs #PARSING_FILES #EXECUTION_FILES
    rustc -o $t $d #RUST_FLAGS -g

clean:
    rm -f #BIN_FILE
