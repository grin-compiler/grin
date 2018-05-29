reset
stack exec -- grin -o . -p \
  --bn --cnp --hpt \
  --save-llvm simple --llvm \
  --cse --cp --dve \
  --ar --cp --dve \
  --hpt \
  --ar --cp --dve \
  --hpt -p --llvm \
  --save-llvm opt --save-grin opt.grin --ast \
  stage-06.grin
