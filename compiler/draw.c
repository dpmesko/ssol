//-------------------------------------------------------
//  ssol/draw.c
//			This is the main C file for our SVG file rendering 
//			utility, to be linked in with the LLVM code 
//			generated by the SSOL compiler.
//
//			This code is mostly not ours! The original
//			source code was taken from here:
//				http://www.code-in-c.com/writing-svg-library-c/
//
//			See also svg.h and svg.c
//
//-------------------------------------------------------

#include<stdio.h>
#include<math.h>
#include<time.h>

#include "svg.h"

#define POINT 0

//--------------------------------------------------------
// FUNCTION PROTOTYPES
//--------------------------------------------------------
void read_canvas(struct canvas_node *node, svg *psvg);
void draw(struct canvas *canv, char *filename);

// --------------------------------------------------------
// FUNCTIONS
// --------------------------------------------------------

/*
int main()
{
		struct point pt1, pt2, pt3, pt4;
		struct point pt5, pt6, pt7, pt8;
		pt1.x = 100;
		pt1.y = 300;
		pt2.x = 100;
		pt2.y = 100;
		pt3.x = 250;
		pt3.y = 400;
		pt4.x = 250;
		pt4.y = 200;
		pt5.x = 233;
		pt5.y = 399;
		pt6.x = 166;
		pt6.y = 499;
		pt7.x = 179;
		pt7.y = 404;
		pt8.x = 231;
		pt8.y = 498;

		struct curve cv1, cv2;
		cv1.ep1 = pt1;
		cv1.ep2 = pt2;
		cv1.cp1 = pt3;
		cv1.cp2 = pt4;
		cv2.ep1 = pt5;
		cv2.ep2 = pt6;
		cv2.cp1 = pt7;
		cv2.cp2 = pt8;

		struct canvas_node elem1, elem2;
		elem1.next = &elem2;
		elem1.ct = &cv1;
		elem2.next = NULL;
		elem2.ct = &cv2;

		struct canvas can;
		can.first = &elem1;
		can.x = 500;
		can.y = 500;

		draw(&can, "main.svg");
}*/

void draw(struct canvas *canv, char *filename)
{
		svg* psvg;
		psvg = svg_create(canv->x, canv->y);

		if (psvg == NULL) {
				fprintf(stderr, "could not store SVG meta data, malloc returned null");
				exit(1);
		}
		else{
			read_canvas(canv->first, psvg);
			
			svg_finalize(psvg);
			svg_save(psvg, filename);
			svg_free(psvg);
		}
}


void read_canvas(struct canvas_node *node, svg *psvg)
{	
		// Check what the canvas node points to and then
		//  call the appropriate rendering function 
		
		struct canvas_node *next = node->next;
		struct curve *ct = node->ct;

		int ep1x = (int) ct->ep1.x;
		int ep1y = (int) ct->ep1.y;
		int ep2x = (int) ct->ep2.x;
		int ep2y = (int) ct->ep2.y;
		int cp1x = (int) ct->cp1.x;
		int cp1y = (int) ct->cp1.y;
		int cp2x = (int) ct->cp2.x;
		int cp2y = (int) ct->cp2.y;

		svg_bezier(psvg, ep1x, ep1y, ep2x, ep2y, cp1x, cp1y, cp2x, cp2y); 
		
		// the recursive call. Should be prefix or postfix?
		if (next != NULL)
			read_canvas(next, psvg);

}

