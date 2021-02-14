if exists("g:treesitter_enabled")
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",     -- one of "all", "language", or a list of languages

  highlight = {
    enable = true,              -- false will disable the whole extension
  },

  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "+",
      scope_incremental = "+",
      node_decremental = "_",
    },
  },

  -- refactor = {
  --   highlight_definitions = { enable = true },
  -- },
}
EOF
endif

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
