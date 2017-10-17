{ coreutils, git, writeDashBin }:

writeDashBin "git-preview" ''
  set -efu
  head_commit=$(${git}/bin/git log -1 --format=%H)
  merge_commit=$1; shift
  merge_message='Merge for git-preview'
  preview_dir=$(${coreutils}/bin/mktemp --tmpdir -d git-preview.XXXXXXXX)
  preview_branch=$(${coreutils}/bin/basename "$preview_dir")
  ${git}/bin/git worktree add -b "$preview_branch" "$preview_dir" >/dev/null
  ${git}/bin/git -C "$preview_dir" checkout "$head_commit"
  ${git}/bin/git -C "$preview_dir" merge -m "$merge_message" "$merge_commit"
  ${git}/bin/git -C "$preview_dir" diff "$head_commit.." "$@" &
  ${git}/bin/git branch -fd "$preview_branch"
  ${coreutils}/bin/rm -fR "$preview_dir"
  wait
''
