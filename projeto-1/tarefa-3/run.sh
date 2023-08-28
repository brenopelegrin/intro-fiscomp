rm remake.bin
rm input.data
rm output.data
python3 input_gen.py
f77 remake.f -o remake.bin
./remake.bin
