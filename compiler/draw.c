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
void render_curve(struct curve *ct, svg *psvg);

// --------------------------------------------------------
// FUNCTIONS
// --------------------------------------------------------

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

		render_curve(node->ct, psvg);
		
		// the recursive call. Should be prefix or postfix?
		if (next != NULL)
			read_canvas(next, psvg);

}


void render_curve(struct curve *ct, svg *psvg)
{


}

