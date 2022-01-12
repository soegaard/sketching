/*
 * Original: https://github.com/processing/p5.js-website/blob/main/src/data/examples/en/08_Math/05_sincosine.js
 * @name Sine Cosine
 * @arialabel Two blue and two yellow circles move side to side on each side of a white square
 * @description Linear movement with sin() and cos().
 * Numbers between 0 and 2π (2π which angles roughly 6.28)
 * are put into these functions and numbers between -1 and 1 are returned.
 * These values are then scaled to produce larger movements.
 */
let angle1 = 0;
let angle2 = 0;
let scalar = 70;

function setup() {
  createCanvas(710, 400);
  noStroke();
  rectMode(CENTER);
}

function draw() {
  background(0);

  let ang1 = radians(angle1);
  let ang2 = radians(angle2);

  let x1 = width / 2 + scalar * cos(ang1);
  let x2 = width / 2 + scalar * cos(ang2);

  let y1 = height / 2 + scalar * sin(ang1);
  let y2 = height / 2 + scalar * sin(ang2);

  fill(255);
  rect(width * 0.5, height * 0.5, 140, 140);

  fill(0, 102, 153);
  ellipse(x1, height * 0.5 - 120, scalar, scalar);
  ellipse(x2, height * 0.5 + 120, scalar, scalar);

  fill(255, 204, 0);
  ellipse(width * 0.5 - 120, y1, scalar, scalar);
  ellipse(width * 0.5 + 120, y2, scalar, scalar);

  angle1 += 2;
  angle2 += 3;
}
