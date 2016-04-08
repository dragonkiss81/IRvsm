#include <iostream>
#include <fstream>
#include <vector>
#define FILE_LIST_NUM 46972
using namespace std;

int main() {
    ifstream fin("test/inverted-file");
    ofstream fout("out/reshape.txt");
    ofstream fout2("out/wordvec.txt");
    if(!fin.good()) return 1;

    int vocab_count = 1;
    int vocab_id_1, vocab_id_2, num_contain;
    int file_id, file_count;
    while (!fin.eof()){
        fin>>vocab_id_1>>vocab_id_2>>num_contain;
	    if( 5396<=vocab_id_1 && vocab_id_1<= 12277 && num_contain > 1000){
	        fout2<<vocab_id_1<<" "<<vocab_id_2<<endl;

            for(int i=0; i<num_contain; i++){
                fin>>file_id>>file_count;
                fout<<vocab_count<<","<<file_id+1<<","<<file_count<<endl;
            }
            vocab_count++;
        }
        else{
            for(int i=0; i<num_contain; i++){ fin>>file_id>>file_count; } //skip
        }
    }

    return 0;
}
