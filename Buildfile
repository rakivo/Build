SRC_DIR=src
BIN_FILE=buildfile
RUST_FLAGS=--edition 2021
PARSING_FILES=#SRC_DIR/parsing/ast.rs #SRC_DIR/parsing/lexer.rs #SRC_DIR/parsing/parser.rs

ifdef $RELEASE
    #RUST_FLAGS+=-C opt-level=3
endif

ifdef $DEBUG
    #RUST_FLAGS+=--cfg='feature="dbg"'
endif

#BIN_FILE: #SRC_DIR/main.rs #PARSING_FILES
    rustc -o $t $d #RUST_FLAGS -g

clean:
    rm -f #BIN_FILE
