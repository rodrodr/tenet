HTMLWidgets.widget({

  name: 'plotChord',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

          var root = am5.Root.new(el.id);


        // Set themes
        // https://www.amcharts.com/docs/v5/concepts/themes/
        root.setThemes([
          am5themes_Animated.new(root)
        ]);

        // Create series
        // https://www.amcharts.com/docs/v5/charts/flow-charts/
        var series = root.container.children.push(am5flow.ChordDirected.new(root, {
          startAngle: 80,
          padAngle: 2,
          linkHeadRadius: undefined,
          sourceIdField: "from",
          targetIdField: "to",
          valueField: "value",
          nodeWidth: x.node_width,
          radius: am5.percent(x.radius_percent)
        }));

        series.nodes.labels.template.setAll({
          textType: "radial",
          centerX: 0,
          fontSize: x.font_size
        });


        series.links.template.set("fillStyle", "source");

        var linkTemplate = series.links.template;
        linkTemplate.setAll({
        	strokeOpacity: 0,
          fillOpacity: x.opacity
        });

        var hoverState = linkTemplate.states.create("hover", {
        	fillOpacity: 1,
          strokeOpacity: 0});


      // Set data
      // https://www.amcharts.com/docs/v5/charts/flow-charts/#Setting_data
      series.data.setAll(x.data);


      // Make stuff animate on load
      series.appear(1000, 100);


      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
