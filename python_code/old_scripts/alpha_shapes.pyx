#!/usr/bin/env python

from Bio.PDB import *
import numpy as np
import timeit, logging, os, math

from benchmarkers import *
from make_it_simple import get_radius

# this scripts gets alpha shape for given triangulated set of atoms from pdb
# as a union of balls
# uses qhull.org python interface from scipy

# second arg - to test on very-very small subset
# should return triangulation, with some additional objects - neighbours of edge and neighbours of vertex
@timed
def process_chain(points, points_no = 0):
    logging.debug("process_chain called for " + str(points_no) + " atoms")
    print("process_chain called for " + str(points_no) + " atoms")
    from scipy.spatial import Delaunay, KDTree
    p0 = points[: (points_no if points_no > 0 else len(points))]
    p = np.array(map(lambda x : (x.get_vector()._ar), p0))
    tri = Delaunay(p)
    tree = KDTree(p)
    return (p0, tri, tree, p)

@timed
def read_pdb_info(filename, chain1= 'L', chain2 = 'H'):
    parser = PDBParser()
    structure = parser.get_structure('', filename)
    return (
        list(structure[0][chain1].get_atoms()),
        list(structure[0][chain2].get_atoms()) # ,
        #list(map(lambda x : (x.get_vector()._ar), structure[0]['L'].get_atoms())),
        #list(map(lambda x : (x.get_vector()._ar), structure[0]['H'].get_atoms()))
    )

def volume_2(l1_2, l2_2, l3_2, l4_2, l5_2, l6_2):
    return (l1_2 * l5_2 * (l2_2 + l3_2 + l4_2 + l6_2 - l1_2 - l5_2) +
     l2_2 * l6_2 * (l1_2 + l3_2 + l4_2 + l5_2 - l2_2 - l6_2) +
     l3_2 * l4_2 * (l1_2 + l2_2 + l5_2 + l6_2 - l3_2 - l4_2) -
     l1_2 * l2_2 * l4_2 -
     l2_2 * l3_2 * l5_2 -
     l1_2 * l3_2 * l6_2 -
     l4_2 * l5_2 * l6_2
    ) / 144.0
def area_2(a_2, b_2, c_2):
    result = (
     2 * a_2 * b_2 + 2 * a_2 * c_2 + 2 * b_2 * c_2 - a_2 * a_2 - b_2 * b_2 - c_2 * c_2
    ) / 16.0
    if result >= 0.0:
        return math.sqrt(result)
    return -1.0

# calls get_radius(atom_name)
# gets 4 atoms
# returns True if difference between their van der waals spheres and tetrahedra built on their atom centers is zero,
# False - otherwise.
def check_names(a1, a2, a3, a4, coords_method, name_method):
    r1 = get_radius(a1.element) ** 2
    r2 = get_radius(a2.element) ** 2
    r3 = get_radius(a3.element) ** 2
    r4 = get_radius(a4.element) ** 2
    l1 = (a1 - a2) ** 2
    l2 = (a1 - a3) ** 2
    l3 = (a1 - a4) ** 2
    l4 = (a2 - a3) ** 2
    l5 = (a2 - a4) ** 2
    l6 = (a3 - a4) ** 2
    v0 = volume_2(l1, l2, l3, l4, l5, l6)
    v1 = volume_2(l1, l2, l4, r1, r2, r3)
    v2 = volume_2(l1, l3, l5, r1, r2, r4)
    v3 = volume_2(l2, l3, l6, r1, r3, r4)
    v4 = volume_2(l4, l5, l6, r2, r3, r4)
    if v0 <= 0: return False
    return v0 - v1 - v2 - v3 - v4 <= 0.0

def check_triangle(a1, a2, a3, r=1.4):
    r1 = (get_radius(a1.element) + r) ** 2
    r2 = (get_radius(a2.element) + r) ** 2
    r3 = (get_radius(a3.element) + r) ** 2
    l1 = (a1 - a2) ** 2
    l2 = (a2 - a3) ** 2
    l3 = (a1 - a3) ** 2
    s0 = area_2(l1, l2, l3)
    s1 = area_2(l2, r2, r3)
    s2 = area_2(l3, r1, r3)
    s3 = area_2(l1, r1, r2)
    if s0 <= 0: return False
    if s1 <= 0: return True
    if s2 <= 0: return True
    if s3 <= 0: return True
    return s0 - s1 - s2 - s3 >= 0.0

def check_triangle2(a1, a2, a3, r=1.4):
    from chempy import cpv
    r1 = (a1.vdw + r) ** 2
    r2 = (a2.vdw + r) ** 2
    r3 = (a3.vdw + r) ** 2
    l1 = cpv.distance(a1.coord, a2.coord) ** 2
    l2 = cpv.distance(a2.coord, a3.coord) ** 2
    l3 = cpv.distance(a1.coord, a3.coord) ** 2
    s0 = area_2(l1, l2, l3)
    s1 = area_2(l2, r2, r3)
    s2 = area_2(l3, r1, r3)
    s3 = area_2(l1, r1, r2)
    if s0 <= 0: return False
    if s1 <= 0: return True
    if s2 <= 0: return True
    if s3 <= 0: return True
    return s0 - s1 - s2 - s3 >= 0.0

if __name__ == "__main__":
    logging.basicConfig(filename="logs/" + os.path.splitext(os.path.basename(__file__))[0] + ".log", level=logging.DEBUG)
    logging.debug("============\nCalled simple script")
    pair_of_chains = read_pdb_info(os.path.abspath('../test_data/2OSL.pdb'))
    #chains_ss_info = read_dssp_info(os.path.abspath('../test_data/2OSL.pdb'))
    #print chains_ss_info
    surface1 = process_chain(pair_of_chains[0])
    surface2 = process_chain(pair_of_chains[1])
    print(surface1[1].simplices)
    print(area_2(16, 4, 4))
