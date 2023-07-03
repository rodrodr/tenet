HTMLWidgets.widget({

  name: 'plotSankey',

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

        var series = root.container.children.push(am5flow.Sankey.new(root, {
          sourceIdField: "from",
          targetIdField: "to",
          valueField: "value",
          paddingRight: 140
        }));

        series.nodes.get("colors").set("step", 2);
        
        var linkTemplate = series.links.template;
        linkTemplate.setAll({
        	strokeOpacity: 0,
          fillOpacity: 0.05
        });

        var hoverState = linkTemplate.states.create("hover", {
        	fillOpacity: 1,
          strokeOpacity: 0});
        
        series.nodes.labels.template.setAll({
          textType: "radial",
          centerX: 0,
          fontSize:12
        });

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
