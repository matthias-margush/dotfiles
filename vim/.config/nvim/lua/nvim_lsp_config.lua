local api = vim.api
local util = require 'vim.lsp.util'

local M = {}

-- Displays hover doc in the preview window.
function M.doc_preview(_, method, result)
  if not (result and result.contents) then
    -- return { 'No information available' }
    return
  end
  local markdown_lines = util.convert_input_to_markdown_lines(result.contents)
  markdown_lines = util.trim_empty_lines(markdown_lines)
  if vim.tbl_isempty(markdown_lines) then
    -- return { 'No information available' }
    return
  end

  local winnr = api.nvim_command('pclose')
  local previewheight = api.nvim_get_option('previewheight')
  local winnr = api.nvim_call_function('win_getid', {})

  api.nvim_command('silent! ' .. previewheight .. 'new! doc')
  vim.bo[0].buftype = 'nofile'
  vim.bo[0].buflisted = false
  vim.bo[0].swapfile = false
  vim.bo[0].filetype = 'markdown'
  vim.wo[0].conceallevel = 1
  vim.wo[0].number = false
  vim.wo[0].previewwindow = true

  local docnr = api.nvim_win_get_number(0)
  api.nvim_buf_set_lines(docnr, 0, -1, false, markdown_lines)
  api.nvim_call_function('win_gotoid', {winnr})
end

function on_attach(client, bufnr)
  require'diagnostic'.on_attach()
  require'completion'.on_attach()

  -- Set the omnifunc for this buffer.
  api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Configure some mappings for this buffer.
  -- local opts = { noremap=true, silent=true }
  local opts = { noremap=true }
end

function M.quickfix_diagnostics(err, method, result, client_id)
  -- default_callback(err, method, result, client_id)
  if result and result.diagnostics then
    -- for _, v in ipairs(result.diagnostics) do
    --   v.bufnr = client_id
    --   v.lnum = v.range.start.line + 1
    --   v.col = v.range.start.character + 1
    --   v.text = v.message
    -- end
    -- for _, v in ipairs(result.diagnostics) do
    --   v.uri = v.uri or result.uri
    -- end
    for _, diagnostic in ipairs(vim.lsp.util.diagnostics_by_buf) do
      vim.lsp.util.set_qflist(diagnostic)
    end
    -- vim.lsp.util.set_qflist(result.diagnostics)
    -- vim.lsp.util.set_loclist(result.diagnostics)
  end
end

vim.lsp.callbacks['textDocument/hover'] = M.doc_preview
vim.lsp.callbacks['textDocument/publishDiagnostics'] = M.quickfix_diagnostics

nvim_lsp = require("lspconfig")
nvim_lsp.gopls.setup({ on_attach=on_attach })
nvim_lsp.clojure_lsp.setup({ on_attach=on_attach })
nvim_lsp.rls.setup{}

return M
