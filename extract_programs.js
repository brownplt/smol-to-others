var toml = require('toml');

function processTutorial(tutorial) {

    console.log(`Processing tutorial ${tutorial}`);

    function outputProgram(name, program) {
        require('fs').writeFileSync(
            `Programs/${tutorial}_${name}.smol`,
            program);
    }

    require('fs').readFile(`tutorials/${tutorial}.toml`, function (err, data) {
        var json = toml.parse(data);
        for (let [k, v] of Object.entries(json.questions)) {
            program = v["program"];
            answer = v["answer"];
            if (program && answer) {
                outputProgram(k, program);
            }
            again = v["again"];
            if (again) {
                v = again;
                program = v["program"];
                answer = v["answer"];
                if (program && answer) {
                    outputProgram(k + "_again", program);
                }
            }
        }
    });
};

// processTutorial("1-scope");
// processTutorial("2-order");
// The TOML lib is unhappy with the following file. I don't know why.
// processTutorial("3a-more-scope");
// processTutorial("3b-mut-vars");
processTutorial("4-begin");
processTutorial("5a-vectors1");
processTutorial("5b-vectors2");
// processTutorial("6-heap");
// processTutorial("7a-lambda1");
// processTutorial("7b-lambda2");
// processTutorial("7c-lambda3");