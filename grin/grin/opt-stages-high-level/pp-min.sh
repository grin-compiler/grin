stack exec -- grin -p \
  --bn --cnp --compile-hpt --run-hpt-pure \
  --cse --cfl --dve \
  --ar --cfl --dve \
  --compile-hpt --run-hpt-pure \
  --ar --cfl --dve \
  --compile-hpt --run-hpt-pure -p --llvm \
  stage-06.grin
