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
extern "C" double* load_cpp(char imageName[])
{
    Mat img = imread(imageName,CV_LOAD_IMAGE_COLOR);
    unsigned char* input = (unsigned char*)(img.data);
    double* output = new double[2+3*img.rows*img.cols];
    output[0]=img.rows;
    output[1]=img.cols;
    double r,g,b;
    int k = 2;
    for(int i = 0;i < img.rows;i++){
        for(int j = 0;j < img.cols;j++){
            b = input[img.step * i + j*img.channels()] ;
            output[k++]=b;
            g = input[img.step * i + j*img.channels() + 1];
            output[k++]=g;
            r = input[img.step * i + j*img.channels() + 2];
            output[k++]=r;
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
    return output;
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