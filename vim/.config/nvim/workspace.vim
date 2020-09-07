if !exists("g:skip_session")
  set sessionoptions-=blank
  let g:workspace_autosave_untrailspaces = 0
  let g:workspace_session_disable_on_args = 1
  let g:workspace_persist_undo_history = 0
  let g:workspace_autocreate = 1
  let g:workspace_autosave = 0
  let g:workspace_session_name = '.session.vim'
  let g:workspace_autosave_ignorelist = ['gitcommit', 'gitrebase', 'nerdtree', 'qf']
endif
