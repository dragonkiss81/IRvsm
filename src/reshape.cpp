#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#define FILE_LIST_NUM 46972
using namespace std;

int main() {
    ifstream fin("inverted-file");
    ofstream fout("reshape_one.txt");
    if(!fin.good()) return 1;

    int vocab_count = 1;
    while (!fin.eof()){
        int vocab_id_1, vocab_id_2, num_contain;
        fin>>vocab_id_1>>vocab_id_2>>num_contain;

        int file_id, file_count;
        map<int, int> file_to_count;
        map<int, int>::iterator iter;

        for(int i=0; i<num_contain; i++){
            fin>>file_id>>file_count;
            file_to_count[file_id] = file_count;
        }

        for(int i=1; i<=FILE_LIST_NUM; i++){
            iter = file_to_count.find(i);
            if(iter != file_to_count.end()){
                fout<<vocab_count<<","<<i<<","<<iter->second<<endl;
            }
        }
        
        vocab_count++;
    }
    
    return 0;
}
