#include <iostream>
#include <fstream>
#include <vector>
#include <math.h>
#define FILE_LIST_NUM 46972
using namespace std;

int main() {
    ifstream fin("test/inverted-file");
    ofstream fout("out/reshape.txt");
    ofstream fout_wvec("out/wordvec.txt");
    if(!fin.good()) return 1;

    int vocab_id_1, vocab_id_2, num_contain;
    int file_id, file_count;

    vector<int> df_list;
    int ws_list[FILE_LIST_NUM] = {0};
    int vocab_count = 1;
    double idf_check;
    while (!fin.eof()){
        fin>>vocab_id_1>>vocab_id_2>>num_contain;
        //idf_check = double(log2( (FILE_LIST_NUM - num_contain + 0.5) / (num_contain + 0.5)));
        //cout<<idf_check<<endl;
	    if( 5396<=vocab_id_1 && vocab_id_1<= 12277 && num_contain > 80){
            fout_wvec<<vocab_id_1<<" "<<vocab_id_2<<endl;
            df_list.push_back(num_contain);

            for(int i=0; i<num_contain; i++){
                fin>>file_id>>file_count;
                ws_list[file_id] = ws_list[file_id] + file_count; //for ws
                fout<<vocab_count<<" "<<file_id+1<<" "<<file_count<<endl;
            }
            vocab_count++;
        }
        else{
            for(int i=0; i<num_contain; i++){ fin>>file_id>>file_count; } //skip
        }
    }

    // for(int i=0; i<FILE_LIST_NUM; i++){
    //     cout<<ws_list[i]<<" ";
    // }
    // cout<<endl;

    // for(int i=0; i<df_list.size(); i++){
    //     cout<<df_list[i]<<" ";
    // }
    // cout<<endl;

    ifstream fin_spar("out/reshape.txt");
    ofstream fout_rev("out/rev.txt");

    int row, col;
    double tf, idf;
    double value;
    double rev_value;
    while(!fin_spar.eof()){
        fin_spar>>row>>col>>value;
        // cout<<value/ws_list[col-1]<<" "<<double(log2(FILE_LIST_NUM/df_list[row-1])+1)<<endl;
        tf = double(value/ws_list[col-1]);
        idf = double(log2( (FILE_LIST_NUM - df_list[row-1] + 0.5) / (df_list[row-1] + 0.5)));
        rev_value =  tf * idf;
        fout_rev<<row<<","<<col<<","<<rev_value<<endl;
    }

    return 0;
}
