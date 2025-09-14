return {
  -- Markdown support
  {
    "godlygeek/tabular",
    ft = "markdown",
    cmd = "Tabularize",
  },
  {
    "plasticboy/vim-markdown",
    ft = "markdown",
    dependencies = { "godlygeek/tabular" },
  },
  {
    "mzlogin/vim-markdown-toc",
    ft = "markdown",
    cmd = {
      "GenTocGFM",
      "GenTocRedcarpet",
      "GenTocGitLab",
      "UpdateToc",
    },
  },
  {
    "kannokanno/previm",
    ft = "markdown",
    cmd = "PrevimOpen",
  },
}