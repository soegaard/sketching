/**
 * Distance 1D.
 *
 * Move the mouse left and right to control the
 * speed and direction of the moving shapes.
 *
 * From https://processing.org/examples/distance1d.html
 */

function runLiveSketch(s) {
  var xpos1;
  var xpos2;
  var xpos3;
  var xpos4;
  var thin = 8;
  var thick = 36;

  s.setup = () => {
    s.createCanvas(640, 360);
    s.noStroke();
    xpos1 = s.width / 2;
    xpos2 = s.width / 2;
    xpos3 = s.width / 2;
    xpos4 = s.width / 2;
  };

  s.draw = () => {
    s.background(0);

    var mx = s.mouseX * 0.4 - s.width / 5.0;

    s.fill(102);
    s.rect(xpos2, 0, thick, s.height / 2);
    s.fill(204);
    s.rect(xpos1, 0, thin, s.height / 2);
    s.fill(102);
    s.rect(xpos4, s.height / 2, thick, s.height / 2);
    s.fill(204);
    s.rect(xpos3, s.height / 2, thin, s.height / 2);

    xpos1 += mx / 16;
    xpos2 += mx / 64;
    xpos3 -= mx / 16;
    xpos4 -= mx / 64;

    if (xpos1 < -thin) {
      xpos1 = s.width;
    }
    if (xpos1 > s.width) {
      xpos1 = -thin;
    }
    if (xpos2 < -thick) {
      xpos2 = s.width;
    }
    if (xpos2 > s.width) {
      xpos2 = -thick;
    }
    if (xpos3 < -thin) {
      xpos3 = s.width;
    }
    if (xpos3 > s.width) {
      xpos3 = -thin;
    }
    if (xpos4 < -thick) {
      xpos4 = s.width;
    }
    if (xpos4 > s.width) {
      xpos4 = -thick;
    }
  };
}
