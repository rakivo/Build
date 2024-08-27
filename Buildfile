DEV_DIR=dev
SRC_DIR=src
BIN_FILE=build
SRC_FILES=shell(ls #SRC_DIR/parsing/*.rs) shell(ls #SRC_DIR/execution/*.rs)
MAIN=main.rs
EVAL=eval.rs

RUSTFLAGS=-C opt-level=3

buildfile: #BIN_FILE

ifdef $DEV
    export RUSTFLAGS

    ifdef $RUN
        WHAT=run
    else
        WHAT=build
    endif

    #BIN_FILE: #SRC_DIR/main.rs #SRC_FILES
        cargo #WHAT --manifest-path = #DEV_DIR/Cargo.toml --features=dbg
        @rm $t && mv ./dev/target/debug/$t $t
else
    ifdef $DEBUG
        #RUSTFLAGS+=--cfg='feature="dbg"'
    endif

    #RUSTFLAGS+=--edition 2021
    #BIN_FILE: #SRC_DIR/main.rs #SRC_FILES
        rustc -o $t $d #RUSTFLAGS -g
endif

clean:
    rm -rf #BIN_FILE #DEV_DIR/target
