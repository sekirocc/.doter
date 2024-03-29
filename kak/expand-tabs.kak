define-command noexpandtab %{
    remove-hooks global noexpandtab
    hook -group noexpandtab global NormalKey <gt> %{ try %{
        execute-keys -draft "<a-x>s^\h+<ret><a-@>"
    }}
    set-option global aligntab true
    remove-hooks global expandtab
    remove-hooks global smarttab
}

define-command expandtab %{
    remove-hooks global expandtab
    hook -group expandtab global InsertChar '\t' %{ execute-keys -draft h@ }
    hook -group expandtab global InsertKey <backspace> %{ try %{
        execute-keys -draft <a-h><a-k> "^\h+.\z" <ret>I<space><esc><lt>
    }}
    set-option global aligntab false
    remove-hooks global noexpandtab
    remove-hooks global smarttab
}

define-command smarttab %{
    remove-hooks global smarttab
    hook -group smarttab global InsertKey <tab> %{ try %{
        execute-keys -draft <a-h><a-k> "^\h*.\z" <ret>
    } catch %{
        execute-keys -draft h@
    }}
    hook -group smarttab global NormalKey <gt> %{ try %{
        execute-keys -draft "<a-x>s^\h+<ret><a-@>"
    }}
    set-option global aligntab false
    remove-hooks global expandtab
    remove-hooks global noexpandtab
}


define-command replace-tabs-to-spaces %{
    execute-keys -draft	'%s^\t+<ret>@'
}

define-command replace-spaces-to-tabs %{
    execute-keys -draft	'%s^ +<ret><a-@>'
}

define-command remove-trailing-spaces %{
    execute-keys -draft '\%s\h+$<ret>d'
}

