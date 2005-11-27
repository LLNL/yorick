if (!is_func(streak)) {
  autoload, "drat.i", streak, snap, streak_save, B_nu, B_nu_bar;
  autoload, "drat.i", form_mesh, set_tolerances, track_rays;
  autoload, "drat.i", guess_symmetry, reset_options, get_std_limits;
  autoload, "drat.i", streak_times, gaussian_gate, is_present;
}
