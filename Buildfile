SRC_DIR=src
BIN_FILE=buildfile
PARSING_FILES=#SRC_DIR/parsing/ast.rs #SRC_DIR/parsing/lexer.rs #SRC_DIR/parsing/parser.rs
EXECUTING_FILES=#SRC_DIR/execution/cmd.rs #SRC_DIR/execution/flag.rs #SRC_DIR/execution/flags.rs

RUST_FLAGS=--edition 2021 -C opt-level=3

ifdef $DEBUG
    #RUST_FLAGS-=-C opt-level=3
    #RUST_FLAGS+=--cfg='feature="dbg"'
endif

#BIN_FILE: #SRC_DIR/main.rs #PARSING_FILES #EXECUTING_FILES
    @echo #HELLO
    rustc -o $t $d #RUST_FLAGS -g

clean:
    rm -f #BIN_FILE
