rm -rf *.exe
rm -rf *.dat
rm -rf *.csv
gfortran -Wall tarefa-d-13687303.f -o tarefa-d-13687303.exe
./tarefa-d-13687303.exe

g_flag=''
b_flag=''
verbose='false'

print_usage() {
  printf "\nUsage:\n-g for cleaning pdfs and running analise.ipynb\n"
}

while getopts 'gb:v' flag; do
  case "${flag}" in
    g) g_flag='true';
       printf "Cleaning pdfs and running analise.ipynb...\n";
       rm -rf *.pdf;
       ipython -c "%run analise.ipynb" ;;
    b) b_flag='true' ;;
    v) verbose='true' ;;
    *) print_usage
       exit 1 ;;
  esac
done
