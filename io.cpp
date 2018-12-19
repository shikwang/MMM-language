//============================================================================
// Name        : load.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/opencv.hpp>
#include "opencv2/objdetect/objdetect.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"
 
#include <stdio.h>
#include <iostream>
#include <string>
#include <cmath>

using namespace cv;

using namespace std;
extern "C" void load_cpp(char imageName[],double* mat1,double* mat2,double* mat3)
{
    Mat img = imread(imageName,CV_LOAD_IMAGE_COLOR);
    unsigned char* input = (unsigned char*)(img.data);
    double r,g,b;
    for(int i = 0;i < img.rows;i++){
        for(int j = 0;j < img.cols;j++){
            int k = img.step * i + j*img.channels();
            int s = i*img.cols + j;
            b = input[k] ;
            mat1[s]=b;
            g = input[k + 1];
            mat2[s]=g;
            r = input[k + 2];
            mat3[s]=r;
        }
    }
    
    // cout.precision(17);
    /*
    cout << "input height: " << fixed << output[0] << endl;
    cout << "input width: " << fixed << output[1] << endl;

    cout << "first: " << fixed << output[2] << endl;
    cout << "first: " << fixed << output[3] << endl;
    cout << "first: " << fixed << output[4] << endl;

    cout << "s: " << fixed << output[5] << endl;
    cout << "s: " << fixed << output[6] << endl;
    cout << "s: " << fixed << output[7] << endl;

    cout << "t: " << fixed << output[8] << endl;
    cout << "t: " << fixed << output[9] << endl;
    cout << "t: " << fixed << output[10] << endl;*/
    return;
}

extern "C" void save_cpp(char fileName[],double* mat1,double* mat2,double* mat3,int r, int c) 
{
    int height = r;
    int width = c;
    double* data = new double[3*width*height];

    int ind = 0;
    int i = 0;
    while(i<=3*width*height){
        data[i]=mat1[ind];
        data[i+1]=mat2[ind];
        data[i+2]=mat3[ind];
        i=i+3;
        ind++;
    }

    cout << "f" <<data[0] <<endl;
    cout << "f" <<data[1] <<endl;
    cout << "f" <<data[2] <<endl;

    cout << "f" <<data[3] <<endl;
    cout << "f" <<data[4] <<endl;
    cout << "f" <<data[5] <<endl;

    cout << "f" <<data[6] <<endl;
    cout << "f" <<data[7] <<endl;
    cout << "f" <<data[8] <<endl;

    Mat image = cv::Mat(height, width, CV_64FC3, data);
    imwrite(fileName,image);
    cout << "output width: " << width << endl;
    cout << "output height: " << height << endl;
    return;
}

extern "C" void filter_cpp (double* kernel_d, char inputName[],char outputName[],int d)
{
    Mat dst;
    int ddepth = -1;
    double delta = 0.0;
    Point anchor = Point( -1, -1);

    Mat image = imread(inputName,CV_LOAD_IMAGE_COLOR);

    double k[d][d];
    for(int i=0;i<d;++i){
        for(int j=0;j<d;++j){
            k[i][j] = kernel_d[i*d+j];
        }
    }

    Mat kernel = Mat(d,d,CV_64F,k);

    filter2D(image, dst, ddepth , kernel, anchor, delta, BORDER_DEFAULT);
    imwrite(outputName,dst);
    
    return;
}

extern "C" void iter1mat_cpp (double* indata, double* outdata, double k, int len)
{
    for(int i=0;i<len;++i){
        outdata[i] = indata[i]*k;
    }    
    return;
}

extern "C" void trans_cpp (double* indata, double* outdata, int r, int c)
{
    for(int i=0;i<r;++i){
        for(int j=0;j<c;++j){
            outdata[j*r+i] = indata[i*c+j];
        }
    }   
    return;
}

extern "C" void iter2mat_cpp (double* indata1, double* indata2, double* outdata, int mode, int len)
{
    switch (mode){
        case 0:
        for(int i=0;i<len;++i){
            outdata[i] = indata1[i] + indata2[i];
        }   
        break;
        case 1:
        for(int i=0;i<len;++i){
            outdata[i] = indata1[i] - indata2[i];
        } 
        break;
        case 2:
        for(int i=0;i<len;++i){
            outdata[i] = indata1[i] * indata2[i];
        } 
        break;
        case 3: 
        for(int i=0;i<len;++i){
            outdata[i] = indata1[i] / indata2[i];
        } 
        break;
    }  
    return;
}

extern "C" void matmul_cpp (double* indata1, double* indata2, double* outdata, int r1, int c1, int r2, int c2)
{
    for(int i=0;i<r1;++i){
        for(int j=0;j<c2;++j){
            int idx = i*c2+j;
            float tmp_sum = 0.0;
            int x = i * c1;
            int y = j;
            while (y < r2*c2){
                tmp_sum += indata1[x]*indata2[y];
                x = x + 1;
                y = y + c2;
            }
            outdata[idx] = tmp_sum;
        }
    }
    return;
}