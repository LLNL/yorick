if (!is_func(hex_mesh)) {
  autoload, "hex.i", hex_mesh, hex_query, make_sphere, hydra_mesh;
  autoload, "hex.i", hex_triang, hex_startflag, hydra_start;
  autoload, "hex.i", hex5_track, hex24f_track, hex_24b_track, reg_track;
  autoload, "hex.i", track_integ, track_reduce, track_combine, track_solve;
  autoload, "hex.i", bi_dir, c_adjust, cs_adjust, pic3_rays, conv3_rays;
}
if (!is_func(h_openb)) {
  autoload, "hydra.i", h_openb;
}
