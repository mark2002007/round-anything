// Library: minkowski_round.scad
// Version: 1.0
// Author: IrevDev
// Copyright: 2020
// License: MIT

/*
---Modules
round2d(outside radius,internal radius) -> takes 2d children
This module will round 2d any shape

minkowski_round(outsideRadius,internalRadius,enable,envelopecube[x,y,z]) ->
takes 3d children This module will round any 3d shape though it takes a long
time, possibly more than 12 hours (use a low $fn value, 10-15 to begin with) so
make sure enable is set to 0 untill you are ready for final render,the
envelopecube must be bigger than the object you are rounding, default values are
[500,500,500].

minkowski_outside_round(radius,enable,envelopecube[x,y,z])
minkowski_inside_round(radius,enable,envelopecube[x,y,z])
Both this modules do the same thing as minkowski_round() but focus on either
inside or outside radiuses, which means these modules use one less minkowski()
(two instead of three) making them quicker if you don't want both inside and
outside radiuses.

--Examples
*/

// round2d(1,6)difference(){square([20,20]);square([10,10]);}
// minkowski_round(3,1.5,1)theshape();
// minkowski_inside_round(3,1)theshape();
// minkowski_outside_round(3,1)theshape();

//---good test child for examples
// module theshape(){//example shape
//    difference(){
//        cube([20,20,20]);
//        cube([10,10,10]);
//    }
//}

// $fn=20;
// minkowski_round(0.7,1.5,1,[50,50,50])union(){//--example in the thiniverse
// thumbnail/main image
//   cube([6,6,22]);
//   rotate([30,45,10])cylinder(h=22,d=10);
// }//--I rendered this out with a $fn=25 and it took more than 12 hours on my
// computer

module round2d(OR = 3, IR = 1) {
  offset(OR) {
    offset(-IR - OR) {
      offset(IR) { children(); }
    }
  }
}

module minkowski_round(OR = 1, IR = 1, enable = 1,
                       boundingEnvelope = [ 500, 500, 500 ]) {
  if (enable == 0) { // do nothing if not enabled
    children();
  } else {
    minkowski() {    // expand the now positive shape back out
      difference() { // make the negative shape positive again
        cube(boundingEnvelope - [ 0.1, 0.1, 0.1 ], center = true);
        minkowski() {    // expand the negative shape inwards
          difference() { // create a negative of the children
            cube(boundingEnvelope, center = true);
            minkowski() { // expand the children
              children();
              sphere(IR);
            }
          }
          sphere(OR + IR);
        }
      }
      sphere(OR);
    }
  }
}

module minkowski_outside_round(r = 1, enable = 1,
                               boundingEnvelope = [ 500, 500, 500 ]) {
  if (enable == 0) { // do nothing if not enabled
    children();
  } else {
    minkowski() {    // expand the now positive shape
      difference() { // make the negative positive
        cube(boundingEnvelope - [ 0.1, 0.1, 0.1 ], center = true);
        minkowski() {    // expand the negative inwards
          difference() { // create a negative of the children
            cube(boundingEnvelope, center = true);
            children();
          }
          sphere(r);
        }
      }
      sphere(r);
    }
  }
}

module minkowski_inside_round(r = 1, enable = 1,
                              boundingEnvelope = [ 500, 500, 500 ]) {
  if (enable == 0) { // do nothing if not enabled
    children();
  } else {
    difference() { // make the negative positive again
      cube(boundingEnvelope - [ 0.1, 0.1, 0.1 ], center = true);
      minkowski() {    // expand the negative shape inwards
        difference() { // make the expanded children a negative shape
          cube(boundingEnvelope, center = true);
          minkowski() { // expand the children
            children();
            sphere(r);
          }
        }
        sphere(r);
      }
    }
  }
}