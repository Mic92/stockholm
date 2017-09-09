{ pkgs, ... }:

{
  krebs.newsbot-js.news = {
    feeds = pkgs.writeText "feeds" ''
      antirez|http://antirez.com/rss|#news
      archlinux|http://www.archlinux.org/feeds/news/|#news
      ethereum|http://blog.ethereum.org/feed|#news
      LtU|http://lambda-the-ultimate.org/rss.xml|#news
      mongrel2_master|https://github.com/zedshaw/mongrel2/commits/master.atom|#news
      painload|https://github.com/krebscode/painload/commits/master.atom|#news
      reddit_haskell|http://www.reddit.com/r/haskell/.rss|#news
      reddit_nix|http://www.reddit.com/r/nixos/.rss|#news
      shackspace|http://blog.shackspace.de/?feed=rss2|#news
      tinc|http://tinc-vpn.org/news/index.rss|#news
      vimperator|https://sites.google.com/a/vimperator.org/www/blog/posts.xml|#news
      weechat|http://dev.weechat.org/feed/atom|#news
      xkcd|https://xkcd.com/rss.xml|#news
    '';
  };
}
