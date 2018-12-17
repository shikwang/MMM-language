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
    cout << "width: " << fixed << output[0] << endl;
    cout << "height: " << fixed << output[1] << endl;

    return output;
}

extern "C" void save_cpp(double* input, char fileName[]) 
{
    int height = input[0];
    int width = input[1];
    double* data = new double[3*width*height];
    for(int i = 0; i < 3*width*height; i++) data[i]=input[i+2];
    Mat image = cv::Mat(height, width, CV_64FC3, data);
    imwrite(fileName,image);
    return;
}


// int main()
// {
//     Mat img = imread("/Users/weimansun/Documents/1.jpg",CV_LOAD_IMAGE_COLOR);
//     unsigned char* input = (unsigned char*)(img.data);
//     double* output = new double[2+3*img.rows*img.cols];
//     output[0]=img.cols;//width
//     output[1]=img.rows;//height
//     double r,g,b;
//     int k = 2;
//     for(int i = 0;i < img.rows;i++){
//         for(int j = 0;j < img.cols;j++){
//             b = input[img.step * i + j*img.channels()] ;
//             output[k++]=b;
//             g = input[img.step * i + j*img.channels() + 1];
//             output[k++]=g;
//             r = input[img.step * i + j*img.channels() + 2];
//             output[k++]=r;
//         }
//     }
//     int width = output[0];
//     int height = output[1];
//     double* data = new double[3*width*height];
//     for(int i = 0; i < 3*width*height; i++) data[i]=output[i+2];
//     Mat image = cv::Mat(height, width, CV_64FC3, data);
//     imwrite("/Users/weimansun/Documents/result.jpg",image);
// 	return 0;
// }

