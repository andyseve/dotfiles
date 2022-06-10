{ pkgs, config, lib, ... }: {
  hardware.pulseaudio.enable = false;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    config.pipewire-pulse = {
      "context.properties" = {
        "log.level" = 2;
      };
      "context.modules" = [
        {
          "name" = "libpipewire-module-rtkit";
          "args" = {
            "nice.level" = -15;
            "rt.prio" = 88;
            "rt.time.soft" = 200000;
            "rt.time.hard" = 200000;
          };
          "flags" = [ "ifexists" "nofail" ];
        }
        { "name" = "libpipewire-module-protocol-native"; }
        { "name" = "libpipewire-module-client-node"; }
        { "name" = "libpipewire-module-adapter"; }
        { "name" = "libpipewire-module-metadata"; }
        {
          "args" = {
            "pulse.min.req" = "32/48000";
            "pulse.default.req" = "32/48000";
            "pulse.max.req" = "32/48000";
            "pulse.min.quantum" = "32/48000";
            "pulse.max.quantum" = "32/48000";
            "server.address" = [ "unix:native" ];
          };
          "name" = "libpipewire-module-protocol-pulse";
        }
      ];
      "context.properties" = { };
      "context.spa-libs" = {
        "audio.convert.*" = "audioconvert/libspa-audioconvert";
        "support.*" = "support/libspa-support";
      };
      "stream.properties" = {
        "node.latency" = "32/48000";
        "resample.quality" = 1;
      };
    };
    media-session.config.bluez-monitor = {
      properties = {
        "bluez5.codecs" = [ "sbc" "aac" "ldac" "aptx" "aptx_hd" ];
        "bluez5.mdbc-support" = true;
      };
      rules = [
        {
          actions = {
            update-props = {
              "bluez5.auto-connect" = [ "hsp_hs" "hfp_hf" "a2dp_sink" ];
              "bluez5.hw-volume" =
                [ "hsp_ag" "hfp_ag" "a2dp_source" "a2dp_sink" ];
              "bluez5.autoswitch-profile" = true;
            };
          };
          matches = [{ "device.name" = "~bluez_card.*"; }];
        }
        {
          actions = { update-props = { "node.pause-on-idle" = false; }; };
          matches = [
            { "node.name" = "~bluez_input.*"; }
            { "node.name" = "~bluez_output.*"; }
          ];
        }
      ];
    };
  };
}
