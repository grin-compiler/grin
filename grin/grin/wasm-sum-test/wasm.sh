set -x -e

# simple

stack exec -- grin sum_simple.grin -o . -t --save-llvm sum_simple
#stack exec -- grin debug-mem.grin -o . -t --save-llvm sum_simple

LL=001.sum_simple.ll

sed -i -e 's/i64/i32/g' $LL

opt-7 -O0 $LL | llc-7 -O0 -march=wasm32 -filetype=obj -o simple.wasm

wasm-ld-7 simple.wasm --allow-undefined-file=rts-functions -o simple-linked.wasm --no-entry --initial-memory=67174400 -z 16777216 --stack-first

wasm2wat simple.wasm -o simple.wat

wasm2wat simple-linked.wasm -o simple-linked.wat

# opt

stack exec -- grin sum_simple.grin -o .

LL=033.high-level-opt-code.ll

sed -i -e 's/i64/i32/g' $LL

opt-7 -O3 $LL | llc-7 -O3 -march=wasm32 -filetype=obj -o opt.wasm

wasm-ld-7 opt.wasm --allow-undefined-file=rts-functions -o opt-linked.wasm --no-entry --initial-memory=67174400 -z 16777216 --stack-first

wasm2wat opt.wasm -o opt.wat

wasm2wat opt-linked.wasm -o opt-linked.wat
