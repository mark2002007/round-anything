// extrude_with_radius example

include <Round-Anything-1.0.4/polyround.scad>

radiiPoints=[
    [-4,  0,  1   ],
    [5,   3,  1.5 ],
    [0,   7,  0.1 ],
    [8,   7,  10  ],
    [20,  20, 0.8 ],
    [10,  0,  10  ]
];
extrude_with_radius(3,0.5,0.5,5)polygon(polyround(radiiPoints,30));
