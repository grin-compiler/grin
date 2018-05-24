reset
stack exec -- grin -o . -p \
  --ie --dpe --bn -t \
  --sco --ece --tce -t \
  --gub -t --cp \
  --li --dpe --bn -t \
  --cp --dve --ch -t \
  --bn -t \
  --cse --cp --dve \
  --ar --cp --dve \
  --hpt \
  --ar --cp --dve \
  -p --hpt --llvm \
  stage-00.grin
