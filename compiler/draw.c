#include<stdio.h>
#include<math.h>
#include<time.h>

#include"svg.h"


// STOLEN FROM http://www.code-in-c.com/writing-svg-library-c/


//--------------------------------------------------------
// FUNCTION PROTOTYPES
//--------------------------------------------------------
void drawrectangles(void);
void drawallshapes(void);
void iwanttobelieve(void);
void mondrian(void);

//--------------------------------------------------------
// FUNCTION main
//--------------------------------------------------------
/* int main()
{
    puts("Code in C - SVG\n---------------\n");

    drawrectangles();

		drawhello();
    //drawallshapes();

    //iwanttobelieve();

    //mondrian();

    return EXIT_SUCCESS;
}*/

// --------------------------------------------------------
// FUNCTION drawrectangles
// --------------------------------------------------------


void draw(char *str)
{
		svg* psvg;
		psvg = svg_create(512, 512);

		if (psvg == NULL)
				puts("psvg is NULL");
		else{
			svg_text(psvg, 0, 0, "white", "white", 0, 0, str);
			svg_finalize(psvg);
			svg_save(psvg, "hello.svg");
			svg_free(psvg);
		}
}


void drawrectangles(void)
{
    svg* psvg;
    psvg = svg_create(512, 512);

    if(psvg == NULL)
    {
        puts("psvg is NULL");
    }
    else
    {
        svg_rectangle(psvg, 512, 512, 0, 0, "white", "white", 0, 0, 0);

        svg_rectangle(psvg, 384, 384, 64, 64, "#00FF00", "#000000", 4, 0, 0);
        svg_rectangle(psvg, 320, 320, 96, 96, "#FFFF00", "#000000", 2, 8, 8);
        svg_rectangle(psvg, 256, 256, 128, 128, "#00FFFF", "#000000", 2, 8, 8);
        svg_rectangle(psvg, 192, 192, 160, 160, "#FF80FF", "#000000", 2, 8, 8);

        svg_finalize(psvg);
        svg_save(psvg, "rectangles.svg");
        svg_free(psvg);
    }
}
