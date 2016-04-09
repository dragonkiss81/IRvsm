#include <iostream>
#include <fstream>
#include <vector>
#include <math.h>
#include <string>
#include <string.h>
#define FILE_LIST_NUM 46972
using namespace std;

int fileSize(const char *add){
    ifstream mySource;
    mySource.open(add, ios_base::binary);
    mySource.seekg(0,ios_base::end);
    int size = mySource.tellg();
    mySource.close();
    return size;
}

int GetFileLength(vector<string>& file_list, vector<int>& file_size){
    // get file name list
    ifstream fin_file("test/file-list");
    string temp;
    while(!fin_file.eof()){
        fin_file>>temp;
        file_list.push_back(temp);
    }
    // compute each file size
    char buf[64];
    int size_temp;
    int avg_doc_len = 0;
    for(int i=0; i<file_list.size(); i++){
        strcpy(buf, file_list[i].c_str());
        size_temp = fileSize(buf);
        file_size.push_back(size_temp);
        avg_doc_len = avg_doc_len + size_temp;
        // cout<<file_list[i]<<" "<<file_size<<endl;
    }
    return avg_doc_len/file_list.size();
}


int main() {
    ifstream fin("test/inverted-file");
    ofstream fout("out/reshape.txt");
    ofstream fout_wvec("out/wordvec.txt");

    int vocab_id_1, vocab_id_2, num_contain;
    int file_id, file_count;
    int ws_list[FILE_LIST_NUM] = {0};
    int vocab_count = 1;
    vector<int> df_list;

    while (!fin.eof()){
        fin>>vocab_id_1>>vocab_id_2>>num_contain;
	    if( 5396<=vocab_id_1 && vocab_id_1<= 12277){
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

    vector<string> file_list;
    vector<int> file_size;
    int avg_doc_len = GetFileLength(file_list, file_size);
    // for(int i=0; i<file_list.size(); i++){
    //     cout<<file_list[i]<<" "<<file_size[i]<<endl;
    // }

    ifstream fin_spar("out/reshape.txt");
    ofstream fout_rev("out/rev.txt");

    int row, col;
    double tf, idf, value;
    double bm_tf, bm_rsv;
    double rev_value;
    const int k = 2, b = 0.75;

    while(!fin_spar.eof()){
        fin_spar>>row>>col>>value;
        idf = double(log2( FILE_LIST_NUM / df_list[row-1]));
        //idf = double(log2( (FILE_LIST_NUM - df_list[row-1] + 0.5) / (df_list[row-1] + 0.5)));

        double doc_len_ratio = double(file_size[col-1]/avg_doc_len);
        bm_tf = value / (1 - b + b*doc_len_ratio);
        bm_rsv = idf * (((k+1) * value) / (k*((1-b)+b*doc_len_ratio) + value));

        rev_value =  bm_tf * bm_rsv;
        fout_rev<<row<<","<<col<<","<<rev_value<<endl;
    }

    return 0;
}
