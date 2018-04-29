stack exec -- grin -o . -p \
  --bn --cnp --hpt \
  --save-llvm simple \
  --cse --cfl --dve \
  --ar --cfl --dve \
  --hpt \
  --ar --cfl --dve \
  --hpt -p --llvm \
  --save-llvm opt --ast \
  stage-06.grin
