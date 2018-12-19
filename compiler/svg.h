#include<stdlib.h>
#include<stdbool.h>
#include<stdio.h>
#include<math.h>

// --------------------------------------------------------
// STRUCT definitions
// --------------------------------------------------------
typedef struct svg
{
    char* svg;
    int height;
    int width;
    bool finalized;
} svg;


struct point
{
		double x;
		double y;
};


struct curve
{
		struct point ep1;
		struct point ep2;
		struct point cp1;
		struct point cp2;
};


struct canvas_node
{
		struct canvas_node *next;
		struct curve *ct;
};

struct canvas
{
		float x;
		float y;
		struct canvas_node *first;
};

// --------------------------------------------------------
// FUNCTION PROTOTYPES
// --------------------------------------------------------
svg* svg_create(int width, int height);
void svg_finalize(svg* psvg);
void svg_print(svg* psvg);
void svg_save(svg* psvg, char* filepath);
void svg_free(svg* psvg);
void svg_bezier(svg *psvg, int x1, int y1, int x2, int y2, int cx1, int cy1,
		int cx2, int cy2);
void svg_line(svg* psvg, char* stroke, int strokewidth, int x1, int y1, int x2, int y2);
void svg_fill(svg* psvg, char* fill);
void svg_text(svg* psvg, int x, int y, char* fontfamily, int fontsize, char* fill, char* stroke, char* text);


