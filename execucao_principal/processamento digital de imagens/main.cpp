#include "opencv2/core/core.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"
#include "iostream"
#include "string"
#include "cstring"
#include "fstream"
#include "stdio.h"
#include "stdlib.h"
#include "math.h"

using namespace cv;
using namespace std;

string tostr(const float& t) {
   ostringstream os;
   os<<t;
   return os.str();
}

int main( )
{
    Mat image;
    int cont_lados=0;
    float lado_a_area, lado_a_altura, lado_a_largura, lado_b_area, lado_b_altura, lado_b_largura,volume;
    int classificacao;

    float area_aux;

    string image_name="";
    ofstream arquivo_saida ("dataset2.csv", std::ofstream::out);

    if(!arquivo_saida.is_open())
    {
        cout<<"Falha ao abrir arquivo";
    }
    else
    {
        arquivo_saida << "identificador; lado_a_area; lado_a_altura; lado_a_largura; lado_b_area; lado_b_altura; lado_b_largura; volume; classificacao;\n";

        ///contador para as pastas com as classes
        for(int k=1; k<=4; k++)
        {
            ///contador para as imagens
            for(int l=1; l<=200; l=l+2)
            {
                while(cont_lados<2)
                {
                    stringstream ss;
                    ss<< l + cont_lados;
                    if(k==1)
                    {
                        image_name="imagens/A/A";
                    }
                    else
                    {
                        if(k==2)
                        {
                            image_name="imagens/B/B";
                        }
                        else
                        {
                            if(k==3)
                            {
                                image_name="imagens/E/E";
                            }
                            else
                            {
                                image_name="imagens/J/J";
                            }
                        }
                    }

                    image_name+= ss.str() + ".jpg";
                    image_name= "imagens/B/B1.jpg";
                    cout<<"Nome: "<<image_name<<endl;
                    image = imread(image_name);
                    //imshow( "Display window", image );

                    int largest_area=0;
                    int largest_contour_index=0;
                    Rect bounding_rect;
                    Mat gray;
                    int thresh = 70;

                    ///Conversao para cinza
                    cvtColor(image, gray, CV_BGR2GRAY);
                    //imshow( "Imagem Cinza", gray );

                    ///elemento utilizado para abertura (erosao e dilataçao)
                    Mat element = getStructuringElement(cv::MORPH_CROSS,
                                                        cv::Size(10, 10),
                                                        cv::Point(5, 5));
                    erode(gray, gray,element);
                    dilate(gray, gray, element);

                    //imshow( "Remoçao de Ruidos", gray );

                    ///Limiarizaçao
                    threshold( gray, gray, thresh, 255, THRESH_BINARY );
                    //imshow( "threshold", gray );

                    ///Canny
                    Canny(gray, gray, 100,100);
                    //imshow( "Canny", gray );

                    /// Detecçao de contornos
                    vector<vector<Point> > contours;
                    vector<Vec4i> hierarchy;
                    RNG rng(12345);
                    findContours( gray, contours, hierarchy, CV_RETR_TREE, CV_CHAIN_APPROX_SIMPLE, Point(0, 0) );

                    vector<vector<Point> > contours_poly( contours.size() );
                    for( int i = 0; i < contours.size(); i++ )
                    {
                        approxPolyDP(Mat(contours[i]), contours_poly[i], 5, true );
                    }

                    ///Mescla todos os contornos em um vetor
                    vector<Point> merged_contour_points;
                    for (int i = 0; i < contours_poly.size(); i++)
                    {
                        for (int j = 0; j < contours_poly[i].size(); j++)
                        {
                            merged_contour_points.push_back(contours_poly[i][j]);
                        }
                    }

                    ///obtem a caixa que delimita o objeto
                    vector<Point> hull;
                    convexHull(Mat(merged_contour_points),hull);
                    Mat hull_points(hull);
                    RotatedRect rotated_bounding_rect = minAreaRect(hull_points);

                    /// Desenha os contornos
                    Mat drawing = Mat::zeros( gray.size(), CV_8UC3 );
                    for( int i = 0; i< contours.size(); i++ )
                    {
                        /// Encontra a area do contorno e define a maior area
                        double a=contourArea( contours[i],false);
                        if(a>largest_area)
                        {
                            largest_area=a;
                            /// Armazena o indice do maior contorno
                            largest_contour_index=i;
                            ///Encontra o retângulo delimitador para maior contorno
                            bounding_rect=boundingRect(contours[i]);
                        }
                    }

                    //area_a ou area_b = largest_area
                    if(cont_lados==0)
                    {
                        lado_a_area=largest_area;
                    }
                    else
                    {
                        lado_b_area=largest_area;
                    }

                    Scalar color( 0,255,0);
                    ///encontra a elipse que se encaixa no contorno
                    RotatedRect e = fitEllipse(contours[largest_contour_index]);

                    ///extrai os eixos
                    float a     = e.size.width  / 2;    // width >= height
                    float b     = e.size.height / 2;

                    drawContours( image, contours,largest_contour_index, color, CV_FILLED,8,hierarchy); //desenha o contorno

                    ///Apresenta a imagen com os contornos
                    //imshow( image_name, image );
                    if(cont_lados==0)
                    {
                        lado_a_altura=b;
                        lado_a_largura=a;
                        volume = (4/3) * 3.1415 * a * b * b;
                    }
                    else
                    {
                        lado_b_altura=b;
                        lado_b_largura=a;
                    }

                    cont_lados++;

                }

                //lado_a_area; lado_a_altura; lado_a_largura; lado_b_area; lado_b_altura; lado_b_largura; classificacao
                char identicador[3];
                char classe;
                sprintf(identicador, "%d", (l+1)/2);
                switch(k){
                    case 1: classe='A';   break;
                    case 2: classe='B';   break;
                    case 3: classe='E';   break;
                    case 4: classe='J';   break;
                }

                cout<< classe<<identicador <<";  "<< lado_a_area<<" ;" <<lado_a_altura<<" ;"<< lado_a_largura<<"; " <<lado_b_area<<"; "<< lado_b_altura<<"; "<< lado_b_largura<<"; "<< volume <<"; "<<k<<endl;

                ///escreve no arquivo o dataset
                arquivo_saida<<classe<<identicador<<"; "<<round(lado_a_area)<<"; " <<round(lado_a_altura)<<"; "<<round( lado_a_largura)<<"; " <<round(lado_b_area)<<"; "<<round(lado_b_altura)<<"; "<<round(lado_b_largura)<<"; "<< round(volume)<<"; "<< k<<"\n";
                cont_lados=0;
            }
            waitKey(0);
        }
    }
    arquivo_saida.close();
    return 0;
}
