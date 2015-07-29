{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (pkgs.texLiveAggregationFun { paths = [ pkgs.texLive pkgs.texLiveFull ]; })
  ];
}
